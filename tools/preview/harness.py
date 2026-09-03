"""Shared mechanics behind every `tools/preview_probe.py` family (#2089).

Everything a family owner needs that is NOT a scenario assertion: the
one place a preview engine is launched (`boot_preview`, which is the
only call into `probelib.boot` in the whole preview implementation, so
every launch allocates its own retained log through the single shared
`LOGS` instance — the #1763 contract), the `check` line format, the
`previewManager.dump()` reader, the real key-tap/hold and pointer
helpers, window/framebuffer geometry, the async state poll, the
engine-authoritative trimmed-loading check and its chrome allowlist,
the filesystem-derived simple-category expectation, the forced-replay
oracle both viewers share (#1833), and the derived first grouped item.

`LOGS` is the single `BootLogs` instance every family allocates
through. It is read at call time, never bound at import, so a test may
swap `preview.harness.LOGS` and every owner follows.
"""
from __future__ import annotations

import json
import os
import re
import time
from probelib import boot, send, send_json, poll_until

LOG_DIR = "/tmp"
LOG_PREFIX = "preview_probe_engine"


class BootLogs:
    """One retained engine log per boot, plus the phase->path map (#1763).

    `probelib.boot` opens its log truncating, so two boots pointed at one
    path leave only the last boot's output. This probe boots far more
    than once per phase — `check_units_roster` and
    `check_canonical_dispatch_sweep` both loop, so a full run launches
    one engine per shipped unit and one per swept category on top of the
    fixed phases — and the same TARGET recurs across phases (`icons`,
    `units/acolyte` and `structures/wire` are each browsed more than
    once). A target-derived name therefore cannot be unique; every
    allocation carries its own ordinal as well.

    The mapping is printed the moment a path is handed out, not only in
    the closing summary: `boot` calls `sys.exit` when an engine dies
    before READY, and a failing run is exactly the run whose log a
    reader needs to find.
    """

    def __init__(self, directory: str = LOG_DIR, prefix: str = LOG_PREFIX):
        self._directory = directory
        self._prefix = prefix
        self._allocated: list[tuple[str, str]] = []

    def allocate(self, phase: str) -> str:
        """Reserve (and announce) this boot's own log path."""
        ordinal = len(self._allocated) + 1
        slug = re.sub(r"[^a-z0-9]+", "-", phase.lower()).strip("-") or "boot"
        path = os.path.join(self._directory,
                            f"{self._prefix}_{ordinal:02d}_{slug}.log")
        self._allocated.append((phase, path))
        print(f"  engine log [{ordinal:02d}] {phase}: {path}")
        return path

    def report(self) -> None:
        """Name every log this run wrote, against the phase that wrote it."""
        if not self._allocated:
            print("\nno engine was booted, so this run wrote no engine logs")
            return
        print(f"\nengine logs from this run ({len(self._allocated)} boot"
              f"{'' if len(self._allocated) == 1 else 's'}):")
        for ordinal, (phase, path) in enumerate(self._allocated, start=1):
            print(f"  {ordinal:02d}. {phase}: {path}")


LOGS = BootLogs()


def boot_preview(port: int, phase: str, target: str, label: str):
    """Launch ONE hidden preview engine on ``port`` browsing ``target``.

    The only call into `probelib.boot` in the preview implementation:
    every launch, in every family, allocates its own retained log here
    (#1763) under the ``phase`` string that names it in the closing
    summary, so a family cannot add a boot that falls back to probelib's
    shared per-port default. ``phase`` is also what the log filename's
    slug is derived from, so its spelling is load-bearing.
    """
    return boot(port, log=LOGS.allocate(phase),
                mode=("--preview", target), label=label)

# Every texture scripts.ui.list's list.init() (highlight.png) and its
# scrollbar.init() (arrow buttons + track + the 9-slice scrolltab set,
# scripts/ui/scrollbar.lua + scripts/ui/box_textures.lua) load THE
# MOMENT any list-mode browser is built, regardless of whether that
# particular list ever needs to scroll — the ONE allowed exception to
# "textures within the requested category" (Requirement 5). List mode
# only; focused/item mode never calls assetBrowser.init() at all.
CHROME_TEXTURE_PATHS = frozenset({
    "assets/textures/ui/highlight.png",
    "assets/textures/ui/scrollup.png",
    "assets/textures/ui/scrolldown.png",
    "assets/textures/ui/scrollbar.png",
    "assets/textures/ui/scrollbartop.png",
    "assets/textures/ui/scrollbarbottom.png",
    "assets/textures/ui/scrolltab/scrolltab.png",
    "assets/textures/ui/scrolltab/scrolltabn.png",
    "assets/textures/ui/scrolltab/scrolltabs.png",
    "assets/textures/ui/scrolltab/scrolltabe.png",
    "assets/textures/ui/scrolltab/scrolltabw.png",
    "assets/textures/ui/scrolltab/scrolltabne.png",
    "assets/textures/ui/scrolltab/scrolltabnw.png",
    "assets/textures/ui/scrolltab/scrolltabse.png",
    "assets/textures/ui/scrolltab/scrolltabsw.png",
})


def check(name: str, ok: bool, detail: str = "") -> bool:
    print(f"  [{'PASS' if ok else 'FAIL'}] {name}"
          + (f"  ({detail})" if detail else ""))
    return ok


def dump(port: int):
    got = send_json(port, 'return require("scripts.preview_manager").dump()')
    return got if isinstance(got, dict) else {}


def press_preview_key(port: int, key: str, changed, seconds: float = 10.0) \
        -> tuple[dict, dict]:
    """Tap one real key-down/key-up pair and return two stable observations.

    Release is enqueued immediately after the press, before any debug dump:
    waiting to observe the intermediate state would itself hold the key past
    the repeat delay and turn a tap oracle into an accidental long press.
    """
    quoted = json.dumps(key)
    # One console request is essential: two TCP round trips can themselves
    # exceed the 200 ms repeat delay and no longer describe a tap.
    send(port, f"local d=input.keyDown({quoted}); "
               f"local u=input.keyUp({quoted}); return d and u", timeout=10.0)
    reached = poll_until(seconds, lambda: (
        (lambda state: state if changed(state) else None)(dump(port))))
    reached = reached or dump(port)
    time.sleep(0.15)
    return reached, dump(port)


def hold_preview_key(port: int, key: str, changed, seconds: float = 10.0) \
        -> tuple[dict, dict]:
    """Hold through ``changed``, release, then return held/released states."""
    quoted = json.dumps(key)
    send(port, f"return input.keyDown({quoted})", timeout=10.0)
    try:
        held = poll_until(seconds, lambda: (
            (lambda state: state if changed(state) else None)(dump(port))))
        held = held or dump(port)
    finally:
        send(port, f"return input.keyUp({quoted})", timeout=10.0)
    time.sleep(0.15)
    return held, dump(port)


def window_size(port: int) -> tuple[int, int]:
    """The current WINDOW dimensions (engine.getVideoConfig's vcWidth/
    vcHeight) — the coordinate space engine.setResolution actually
    writes.

    Resize checks below MUST resize relative to these, never to
    previewManager's reported panelBounds: the panel is derived from the
    FRAMEBUFFER, which on a HiDPI display is 2x the window, and is
    further reduced by the browser's margins and list column. Feeding a
    panel height back into setResolution therefore asks for a window far
    larger than intended, so a "shrink" could silently grow the
    framebuffer and leave the visible row count unchanged."""
    got = send_json(port, "local w, h = engine.getVideoConfig(); return {w = w, h = h}")
    if isinstance(got, dict) and got.get("w") and got.get("h"):
        return int(got["w"]), int(got["h"])
    return 800, 600


def framebuffer_size(port: int) -> tuple[int, int]:
    """The current FRAMEBUFFER dimensions — what the browser's layout is
    actually derived from, and (on a HiDPI display) a whole-number
    multiple of the window size window_size() reports."""
    got = send_json(port, "local w, h = engine.getFramebufferSize(); return {w = w, h = h}")
    if isinstance(got, dict) and got.get("w") and got.get("h"):
        return int(got["w"]), int(got["h"])
    return 800, 600


def poll_state(port: int, want: str, seconds: float = 10.0, interval: float = 0.2) -> dict:
    """Poll previewManager.dump() until .state == want (texture upload is
    async — onAssetLoaded lands a tick or two after the request)."""
    deadline = time.monotonic() + seconds
    d = dump(port)
    while d.get("state") != want and time.monotonic() < deadline:
        time.sleep(interval)
        d = dump(port)
    return d


def check_trimmed_loading(port: int, category_root_prefix: str, allow_chrome: bool) -> bool:
    """Requirement 5, verified against the engine's OWN authoritative
    texture-load record (engine.getLoadedTexturePaths — Engine.Asset's
    apAssetPaths, populated by engine.loadTexture's Haskell handler
    itself) rather than any Lua caller's self-reported bookkeeping: every
    loaded texture this whole session is EITHER under the browsed
    category's root OR (list mode only) one of the documented chrome
    assets — no extras, nothing unaccounted for (#886 round-2 review)."""
    loaded = send_json(port, "return engine.getLoadedTexturePaths()")
    loaded = loaded if isinstance(loaded, list) else []
    allowed_chrome = CHROME_TEXTURE_PATHS if allow_chrome else frozenset()
    unaccounted = [p for p in loaded
                   if not p.startswith(category_root_prefix) and p not in allowed_chrome]
    return check("every engine-loaded texture is under the browsed "
                "category's root or a documented chrome asset",
                not unaccounted,
                f"loaded={loaded} unaccounted={unaccounted}")


def check_no_gameplay_scripts_loaded(port: int) -> bool:
    """The normal ~25-script gameplay/menu set (init_loader.lua's
    non-preview branch) never loads in preview mode — the `ui` global it
    wires (require("scripts.ui.registry")) is the cheapest sentinel:
    nil here means that whole branch never ran."""
    result = send(port, "return ui == nil")
    return check("normal gameplay script set never loaded (ui global is nil)",
                 result == "true", result)


def expected_entries_at(root: str) -> list[str]:
    """Independent, filesystem-derived expectation — mirrors
    Engine.Preview.Discovery.discoverEntries's contract (recursive,
    .png only, "/"-joined, sorted) without importing any Haskell/Lua
    code, so this actually cross-checks the real discovery behavior
    rather than restating it. Takes an arbitrary root, because #888
    routes a flora/structures ITEM folder through the very same
    discovery the simple categories use."""
    labels = []
    for dirpath, _dirs, files in os.walk(root):
        rel = os.path.relpath(dirpath, root)
        for f in files:
            if f.lower().endswith(".png"):
                label = f if rel == "." else f"{rel.replace(os.sep, '/')}/{f}"
                labels.append(label)
    return sorted(labels)


def expected_entries(category: str) -> list[str]:
    return expected_entries_at(os.path.join("assets", "textures", category))


def click_element(port: int, bounds: dict) -> None:
    """Click the centre of a dump-reported interactive rect — the
    offscreen_probe.py convention: coordinates ALWAYS come from the
    dump, never from a hardcoded layout guess."""
    x = int(bounds.get("x", 0) + bounds.get("w", bounds.get("width", 0)) / 2)
    y = int(bounds.get("y", 0) + bounds.get("h", bounds.get("height", 0)) / 2)
    send(port, f"return input.click({x}, {y})", timeout=10.0)


def check_forced_replay(port: int, what: str, selected_at: float,
                       frame_count, fps) -> bool:
    """#1833: preview playback REPLAYS every clip, whatever its authored
    `loop` says — the source value stays truthful in the dump, but the
    frame index wraps past the end instead of holding the last frame.

    Deliberately latency-independent, because the tracked `loop: false`
    cycles are short next to a TCP dump round trip (acolyte's
    attack_quick_RH_dagger is 5 frames at 12 fps = 0.417 s, cargo_hold_S's
    demolish 4 at 8 fps = 0.5 s). So this never tries to catch two samples
    inside one cycle, and never demands a strictly decreasing consecutive
    pair. Instead it waits until the clip is provably PAST its own end —
    `selected_at` is taken only after a dump already reported the new
    selection, and both viewers set their clock synchronously inside
    setAnimation/setEntry, so the real clock origin is at or BEFORE it —
    and then requires one sample below the final frame. Under the
    pre-#1833 hold-at-end policy that index is pinned at frameCount-1
    forever once the clip ends, so a single such sample is proof. The
    index must also keep CHANGING, so a clip frozen on some other frame
    cannot pass either.

    fps is read from the dump and guarded: a non-positive effective fps
    has no cycle period at all (frameIndexAt clamps rate to 0 and stays
    on frame 0), so such a fixture is reported unusable rather than
    divided by.
    """
    if not isinstance(frame_count, int) or frame_count < 2 \
            or not isinstance(fps, (int, float)) or fps <= 0:
        return check(f"{what}: replays continuously past its cycle", False,
                     f"unusable fixture: frameCount={frame_count!r} fps={fps!r}")
    cycle = frame_count / float(fps)
    while time.monotonic() - selected_at <= cycle:
        time.sleep(0.05)
    seen: list[int] = []
    deadline = time.time() + max(8.0, cycle * 6)
    while time.time() < deadline:
        idx = (dump(port).get("playback") or {}).get("frameIndex")
        if isinstance(idx, int):
            seen.append(idx)
        if any(i < frame_count - 1 for i in seen) and len(set(seen)) > 1:
            break
        time.sleep(0.05)
    ok_wrap = check(f"{what}: a sample taken past the {cycle:.3f} s cycle "
                    "reports a frame below the last one (impossible while "
                    "holding the end)",
                    any(i < frame_count - 1 for i in seen),
                    f"frameCount={frame_count} fps={fps} samples={seen}")
    ok_moving = check(f"{what}: the index keeps advancing past the cycle "
                      "boundary rather than settling on one frame",
                      len(set(seen)) > 1, f"samples={seen}")
    return ok_wrap and ok_moving


def first_item(category: str) -> str:
    """The first real item directory of a grouped category — derived,
    not hardcoded, so renaming an asset folder can't silently turn this
    into a pre-boot rejection check."""
    root = os.path.join("assets", "textures", category)
    return sorted(d for d in os.listdir(root)
                  if os.path.isdir(os.path.join(root, d))
                  and not os.path.islink(os.path.join(root, d)))[0]
