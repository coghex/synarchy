#!/usr/bin/env python3
"""GUI video/window-settings check (#891) — HUMAN-RUN, needs a windowed instance.

The `render-gpu-asset` capability migration (#891, epic #537) narrowed
`Engine.Graphics.Window.GLFW`, `Engine.Graphics.Vulkan.Swapchain`,
`Engine.Graphics.Vulkan.Recreate` and `Engine.Scripting.Lua.Message.Video`
off unrestricted `EngineEnv` access. None of those paths runs under
`--headless` (no GLFW, no swapchain) and none runs under `--offscreen`
either — `tools/offscreen_probe.py`'s own header says "full Vulkan
render, NO window, no GLFW, no swapchain", so the offscreen probe
proves the Vulkan/UI render path and nothing about *these* modules.
This check covers the gap: it drives the real window-mode / resolution
/ VSync / MSAA / brightness / pixel-snap / texture-filter verbs against
a live GLFW-backed instance and asserts each one round-trips and leaves
the renderer working.

Like `tools/screenshot_check.py` (and per `tools/README.md`'s
GUI-attached convention) this ATTACHES to an already-running GRAPHICAL
instance rather than booting its own — agents never launch a windowed
instance. Launch the game normally (its debug console listens on port
8008), then:

  python3 tools/video_window_check.py            # attach to port 8008
  python3 tools/video_window_check.py --port 9008

It asserts, against the live instance:
  1. engine.getVideoConfig() returns a plausible live config, and
     engine.getWindowSize()/getFramebufferSize() return positive sizes
     (the migrated `API.Config`/`API.Input` reads),
  2. engine.setResolution(w,h) round-trips: the window and framebuffer
     size refs both update (the migrated `Message.Video`
     `handleSetResolution` → GLFW → `rcWindowSizeRef`/
     `rcFramebufferSizeRef` write path),
  3. toggling VSync rebuilds the swapchain and the instance still
     renders a non-degenerate frame afterwards (`Vulkan.Recreate` +
     `Vulkan.Swapchain`),
  4. the same for an MSAA change,
  5. brightness / pixel-snap / texture-filter each apply without
     erroring and leave the instance responsive (`Message.Video`'s
     remaining scalar handlers, plus `Vulkan.Texture.Bindless`'s live
     sampler swap),
  6. a real window-mode TRANSITION runs through
     `handleSetWindowMode` — away from the current mode and back —
     with the instance responsive and `rcWindowSizeRef`/
     `rcFramebufferSizeRef` live and sane through every branch, the
     mode round-tripping, and (from a `windowed` or `borderless` start)
     the window landing back on its exact pre-transition SIZE and
     POSITION. From a `borderless` OR `fullscreen` start it
     additionally asserts that the `windowed` leg reached the SAVED
     resolution — the #1731 / #1882 startup-seed gate. `fullscreen` is
     never chosen as the TARGET of the transition (it switches the
     monitor video mode); `borderless` covers the same code shape
     without disrupting the desktop,
  7. every setting it touched is restored to the value it found — the
     CONFIG resolution and the PHYSICAL window size independently.

Point 7 is not incidental. `engine.setResolution` writes
`vcWidth`/`vcHeight` AND enqueues the GLFW resize, while dragging a
window edge moves only the window — so the two can legitimately
disagree on entry. This script captures both, drives its resize test
from the window size, restores the window with `setResolution`, and
then restores the config with `engine.setVideoConfig` (a config-only
write that enqueues nothing). Both are asserted at the end, so it
cannot report a clean restore while having replaced the user's saved
resolution with a transient window size.

The geometry round trip in point 6 is the #907 regression gate. That bug
made `handleSetWindowMode` decide whether to cache the windowed geometry
by reading `vcWindowMode` — which `API.Config.setWindowModeFn` has
already overwritten with the TARGET mode on the Lua thread — so leaving
`windowed` skipped the cache and returning to `windowed` restored the
borderless monitor geometry instead. The decision now keys off
`wsAppliedMode`, the mode the render thread last actually applied.

Position is read through `debug.getWindowPos()`, the narrow diagnostic
seam added with that fix: `GLFW.getWindowPos` is main-thread-only, so
the Lua thread reads a ref the render thread publishes rather than GLFW
itself. That ref is published on change, not continuously, so this
script forces a publish (a no-op `engine.setResolution`) immediately
before sampling — otherwise a window the human dragged since boot would
be measured where it used to be.

A `borderless` start is the #1731 gate and a `fullscreen` start is the
#1882 one; they are the same gate. `defaultWindowConfig` asks GLFW for
borderless as well as fullscreen, and either mode is applied to the
decorated window `createWindow` just made — so both boots consume the
first-switch caching opportunity, and for both `createWindow` seeds the
windowed cache from that decorated window at the CONFIGURED size,
immediately before mutating it. The `windowed` leg of each round trip
must therefore land on that saved resolution rather than
`defaultWindowState`'s 800x600 fallback.

That mid-transition size is what makes the gate real. `getVideoConfig()`
reports the mode `setWindowModeFn` QUEUED, independently of what the
render thread actually applied, so the mode strings round-trip whether
or not the startup seed is right; only the geometry tells a correctly
seeded `wsAppliedMode` from an incorrect one.

The two starts differ only in what the OUTER round trip can assert. A
`borderless` boot returns to a windowed-geometry-cached state, so its
before/after position and size are both pinned; a `fullscreen` boot's
return leg has no such cache behind it, so its outer geometry stays
reported rather than asserted and only the `windowed` leg is checked.
The `windowed` leg is checked on SIZE alone in both: configuration
persists no position, and the position half of the seed contract is
pinned headlessly by `Graphics.WindowMode`'s `bootPos` fixture instead.

Rendering is verified structurally (the instance keeps answering and
keeps reporting a sane framebuffer). Whether the picture still LOOKS
right after a swapchain rebuild deserves one human eyeball at the
screen — that is the point of this being a GUI-attached check.

Exit code 0 = all checks passed.
"""
from __future__ import annotations

import argparse
import sys
import time

from probelib import send_json


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--port", type=int, default=8008,
                    help="debug-console port of the RUNNING graphical "
                         "instance (default 8008)")
    args = ap.parse_args()
    port = args.port

    failures: list[str] = []

    def check(name: str, ok: bool, detail: str = "") -> None:
        print(f"  [{'ok' if ok else 'FAIL'}] {name}"
              + (f" — {detail}" if detail else ""))
        if not ok:
            failures.append(name)

    def lua(code: str, timeout: float = 10.0):
        return send_json(port, code, timeout=timeout)

    def settle(seconds: float = 1.2) -> None:
        """Give processLuaMessages a few frames to drain and apply."""
        time.sleep(seconds)

    print(f"video_window_check: attaching to port {port}")

    # --- 1. live reads through the migrated capability views ------------
    cfg = lua("local w,h,mode,scale,vs,fl,msaa,bright,snap,filt = "
              "engine.getVideoConfig(); "
              "return {width=w, height=h, mode=mode, scale=scale, "
              "vsync=vs, frameLimit=fl, msaa=msaa, brightness=bright, "
              "pixelSnap=snap, textureFilter=filt}")
    if not isinstance(cfg, dict) or "width" not in cfg:
        print(f"  [FAIL] engine.getVideoConfig() gave {cfg!r}")
        print("  (is a GRAPHICAL instance running on this port? this check "
              "cannot run against --headless or --offscreen)")
        return 1
    check("getVideoConfig returns a live config",
          isinstance(cfg.get("width"), int) and cfg["width"] > 0
          and isinstance(cfg.get("height"), int) and cfg["height"] > 0,
          str(cfg))

    size = lua("local w,h = engine.getWindowSize(); "
               "local fw,fh = engine.getFramebufferSize(); "
               "return {w=w, h=h, fw=fw, fh=fh}")
    ok_size = (isinstance(size, dict)
               and all(isinstance(size.get(k), (int, float)) and size[k] > 0
                       for k in ("w", "h", "fw", "fh")))
    check("getWindowSize/getFramebufferSize are positive", ok_size, str(size))
    if not ok_size:
        print("video_window_check: cannot continue without a live window size")
        return 1

    # TWO independent originals, which can legitimately disagree:
    #   * the CONFIG dimensions (`vcWidth`/`vcHeight`), and
    #   * the PHYSICAL window size right now.
    # Dragging a window edge moves the second without touching the
    # first, and `engine.setResolution` writes BOTH (`API.Config`'s
    # `setResolutionFn` updates the config, then enqueues the GLFW
    # resize). So restoring the window with `setResolution` alone would
    # silently overwrite the user's saved resolution with whatever
    # transient size their window happened to have. Both are captured
    # here and both are restored — and asserted — at the end.
    orig_w, orig_h = int(size["w"]), int(size["h"])
    cfg_w, cfg_h = int(cfg["width"]), int(cfg["height"])
    orig_mode = cfg.get("mode")
    # Restore targets come from the LIVE config, never a hardcoded
    # default: `handleSetBrightness` clamps to 50-300, and a user's
    # persisted config/video.local.yaml can hold any value in range —
    # restoring to a guess would silently change the user's settings.
    orig_scale = float(cfg.get("scale") or 1.0)
    orig_vsync = bool(cfg.get("vsync"))
    orig_flimit = int(cfg.get("frameLimit") or 0)
    orig_msaa = int(cfg.get("msaa") or 1)
    orig_bright = int(cfg.get("brightness") or 100)
    orig_snap = bool(cfg.get("pixelSnap"))
    orig_filter = cfg.get("textureFilter") or "nearest"
    if (cfg_w, cfg_h) != (orig_w, orig_h):
        print(f"    (note: config is {cfg_w}x{cfg_h} but the window is "
              f"currently {orig_w}x{orig_h} — both are restored separately)")

    # --- 2. setResolution round-trips through Message.Video -------------
    # A modest, safely-restorable delta — big enough that a no-op write
    # cannot pass by accident, small enough not to fight the window
    # manager or exceed the monitor.
    new_w, new_h = orig_w - 40, orig_h - 30
    lua(f"engine.setResolution({new_w}, {new_h}); return true")
    settle()
    after = lua("local w,h = engine.getWindowSize(); "
                "local fw,fh = engine.getFramebufferSize(); "
                "return {w=w, h=h, fw=fw, fh=fh}")
    changed = (isinstance(after, dict)
               and (int(after.get("w", orig_w)), int(after.get("h", orig_h)))
                   != (orig_w, orig_h))
    check("setResolution updates the window size ref", changed,
          f"{orig_w}x{orig_h} -> {after}")
    check("framebuffer size ref tracked the resize",
          isinstance(after, dict) and after.get("fw", 0) > 0
          and after.get("fh", 0) > 0, str(after))

    # Restore the PHYSICAL window first; the config dimensions this also
    # clobbers are put back by the config-only write at the end.
    lua(f"engine.setResolution({orig_w}, {orig_h}); return true")
    settle()
    restored = lua("local w,h = engine.getWindowSize(); return {w=w, h=h}")
    check("window size restored",
          isinstance(restored, dict)
          and (int(restored.get("w", -1)), int(restored.get("h", -1)))
              == (orig_w, orig_h),
          str(restored))

    # --- 3/4. swapchain rebuilds (VSync, MSAA) --------------------------
    def alive_and_rendering(label: str) -> None:
        settle()
        alive = send_json(port, "return 1 + 1", timeout=8.0)
        check(f"{label}: instance still responsive", alive == 2, str(alive))
        fb = lua("local fw,fh = engine.getFramebufferSize(); "
                 "return {fw=fw, fh=fh}")
        check(f"{label}: framebuffer still sane",
              isinstance(fb, dict) and fb.get("fw", 0) > 0
              and fb.get("fh", 0) > 0, str(fb))

    lua(f"engine.setVSync({str(not orig_vsync).lower()}); return true")
    alive_and_rendering(f"VSync -> {not orig_vsync} (swapchain rebuild)")
    lua(f"engine.setVSync({str(orig_vsync).lower()}); return true")
    alive_and_rendering(f"VSync -> {orig_vsync} restored (swapchain rebuild)")

    other_msaa = 1 if orig_msaa != 1 else 4
    lua(f"engine.setMSAA({other_msaa}); return true")
    alive_and_rendering(f"MSAA {other_msaa}x (swapchain rebuild)")
    lua(f"engine.setMSAA({orig_msaa}); return true")
    alive_and_rendering(f"MSAA {orig_msaa}x restored (swapchain rebuild)")

    # --- 5. the remaining Message.Video handlers ------------------------
    other_bright = 200 if orig_bright < 150 else 100
    other_filter = "linear" if orig_filter != "linear" else "nearest"
    for verb, apply_, restore in (
        ("brightness", f"engine.setBrightness({other_bright})",
                       f"engine.setBrightness({orig_bright})"),
        ("pixel snap", f"engine.setPixelSnap({str(not orig_snap).lower()})",
                       f"engine.setPixelSnap({str(orig_snap).lower()})"),
        ("texture filter", f'engine.setTextureFilter("{other_filter}")',
                           f'engine.setTextureFilter("{orig_filter}")'),
    ):
        r = lua(f"{apply_}; return true")
        check(f"{verb} applies without error",
              not (isinstance(r, dict) and "error" in r), str(r))
        settle(0.6)
        lua(f"{restore}; return true")
        settle(0.6)

    alive_and_rendering("after all video settings restored")

    # --- 6. a real window-mode transition through handleSetWindowMode ----
    # The remaining migrated `Message.Video` handler, and the only one
    # that touches `rcWindowStateRef`. Re-applying the CURRENT mode would
    # not exercise it meaningfully, so this drives an actual transition
    # away and back.
    #
    # `fullscreen` is never chosen as the target: it switches the
    # monitor's video mode, which is the most disruptive thing this
    # script could do to a human's desktop. `borderless` reaches the same
    # monitor-sized code shape without that. If the instance STARTS in
    # fullscreen or borderless, the round trip goes via `windowed` and
    # the original mode's own branch runs on the way back.
    known_modes = ("windowed", "borderless", "fullscreen")
    mode_state = ("local w,h,mode = engine.getVideoConfig(); "
                  "local ww,wh = engine.getWindowSize(); "
                  "local px,py = debug.getWindowPos(); "
                  "return {mode=mode, winW=ww, winH=wh, posX=px, posY=py}")

    def geometry(state) -> tuple | None:
        """(x, y, w, h) out of a mode_state reply, or None if malformed."""
        if not isinstance(state, dict):
            return None
        try:
            return (int(state["posX"]), int(state["posY"]),
                    int(state["winW"]), int(state["winH"]))
        except (KeyError, TypeError, ValueError):
            return None

    if orig_mode not in known_modes:
        # Never send an unrecognized string back as a "restore" — that
        # would be guessing at the user's window mode.
        check("window mode is one this script can round-trip safely", False,
              f"getVideoConfig reported mode={orig_mode!r}, expected one of "
              f"{known_modes}")
        print("    (skipping the window-mode transition; every other check "
              "still runs)")
    else:
        other_mode = "borderless" if orig_mode == "windowed" else "windowed"

        # `debug.getWindowPos` reports the position as of the render
        # thread's last geometry publish — `GLFW.getWindowPos` is
        # main-thread-only, so the Lua thread cannot sample the window
        # itself. A no-op resize makes the render thread republish, so
        # the baseline below is where the window IS, not where it was
        # when some earlier engine-driven change last moved it.
        lua(f"engine.setResolution({orig_w}, {orig_h}); return true")
        settle()
        before = geometry(lua(mode_state))
        check("pre-transition window geometry is readable", before is not None,
              str(before))

        lua(f'engine.setWindowMode("{other_mode}"); return true')
        alive_and_rendering(f"window mode -> {other_mode}")
        mode_now = lua(mode_state)
        check(f"window mode reports {other_mode}",
              isinstance(mode_now, dict) and mode_now.get("mode") == other_mode,
              str(mode_now))
        check(f"{other_mode}: window size ref stayed positive",
              isinstance(mode_now, dict) and int(mode_now.get("winW") or 0) > 0
              and int(mode_now.get("winH") or 0) > 0, str(mode_now))

        lua(f'engine.setWindowMode("{orig_mode}"); return true')
        alive_and_rendering(f"window mode -> {orig_mode} restored")
        mode_back = lua(mode_state)
        check(f"window mode returns to {orig_mode}",
              isinstance(mode_back, dict)
              and mode_back.get("mode") == orig_mode, str(mode_back))
        check(f"{orig_mode}: window size ref stayed positive",
              isinstance(mode_back, dict)
              and int(mode_back.get("winW") or 0) > 0
              and int(mode_back.get("winH") or 0) > 0, str(mode_back))

        # The #907 regression gate: the round trip must land the window
        # back on the geometry it had before it, POSITION included.
        # `handleSetWindowMode` caches the live windowed pos/size on the
        # way out (keyed off `wsAppliedMode`, the mode the render thread
        # last applied) and restores both on the way back in.
        after = geometry(mode_back)
        mid = geometry(mode_now)
        if orig_mode == "fullscreen":
            # The one start whose OUTER round trip has no windowed cache
            # behind it: `before`/`after` are fullscreen geometry, and
            # the return leg re-enters fullscreen from a windowed window
            # rather than restoring a cached one, so nothing here pins
            # them. The `windowed` leg in between IS asserted, just
            # below — that is #1882's gate.
            print("    (started in 'fullscreen' — outer geometry round "
                  f"trip reported but not asserted: {before} -> {mid} "
                  f"-> {after})")
        else:
            check(f"{orig_mode} start: window-mode round trip restored the "
                  "pre-transition size (#907)",
                  before is not None and after is not None
                  and after[2:] == before[2:],
                  f"{before} -> {after}")
            check(f"{orig_mode} start: window-mode round trip restored the "
                  "pre-transition position (#907)",
                  before is not None and after is not None
                  and after[:2] == before[:2],
                  f"{before} -> {after}")

        if orig_mode in ("borderless", "fullscreen"):
            # #1731 (borderless) and #1882 (fullscreen) — one contract.
            # Such a boot applies its mode at creation, so its first
            # switch to `windowed` is an ENTRY —
            # `applyWindowModeTransition` never caches on the way in, and
            # the only thing that can put the user's resolution in the
            # cache is `createWindow`'s own startup seed. Landing on
            # 800x600 here is exactly the un-seeded failure.
            #
            # Asserted against the CONFIG resolution captured at startup:
            # that is what `defaultWindowConfig` asked GLFW for, and
            # therefore the size of the decorated pre-mutation window the
            # seed was sampled from. It is a proxy, not the cache
            # contract itself — `createWindow` samples the window GLFW
            # actually made, and a window manager may not honour the
            # requested size exactly; `Graphics.WindowMode`'s headless
            # `bootPos`/`bootSize` fixture is what pins the seed
            # authoritatively, position included. Run this from a
            # NON-DEFAULT saved resolution, or the assertion cannot tell
            # the seed from the fallback.
            issue = "#1731" if orig_mode == "borderless" else "#1882"
            check(f"{orig_mode} start: the windowed leg reached the saved "
                  f"resolution rather than the 800x600 fallback ({issue})",
                  mid is not None and mid[2:] == (cfg_w, cfg_h),
                  f"{mid} (saved config resolution {cfg_w}x{cfg_h})")

    # --- 7. leave the instance as we found it ---------------------------
    # Re-pin the physical window size. From a `windowed` or `borderless`
    # start the round trip already restored it (asserted above) and this
    # is a no-op; from a `fullscreen` start there is no windowed-geometry
    # cache guaranteeing it came back to exactly where it began.
    lua(f"engine.setResolution({orig_w}, {orig_h}); return true")
    settle()

    # `engine.setVideoConfig` is the CONFIG-ONLY write (`API.Config`'s
    # `setVideoConfigFn` updates `videoConfigRef` and enqueues nothing),
    # so this restores every one of the ten fields — `vcWidth`/`vcHeight`
    # included, which `setResolution` clobbered just above — without
    # moving the window off the size it was restored to.
    #
    # Skipped entirely on an unrecognized mode: `setVideoConfigFn`
    # validates every argument against the video-config domain and
    # REFUSES the whole ten-field call (returns false, writes nothing)
    # when the window-mode token is unknown (#2198), so passing one
    # through here would restore nothing while reading as a restore.
    # That case is already a recorded failure above; leaving
    # vcWidth/vcHeight alone is the honest outcome.
    if orig_mode in known_modes:
        lua(f'engine.setVideoConfig({cfg_w}, {cfg_h}, "{orig_mode}", '
            f'{orig_scale}, {str(orig_vsync).lower()}, {orig_flimit}, '
            f'{orig_msaa}, {orig_bright}, {str(orig_snap).lower()}, '
            f'"{orig_filter}"); return true')
        settle(0.6)
    else:
        print("    (skipping the config-only restore: refusing to write an "
              f"unrecognized window mode {orig_mode!r})")

    final = lua("local w,h,mode,scale,vs,fl,msaa,bright,snap,filt = "
                "engine.getVideoConfig(); "
                "local ww,wh = engine.getWindowSize(); "
                "return {width=w, height=h, mode=mode, vsync=vs, "
                "frameLimit=fl, msaa=msaa, brightness=bright, "
                "pixelSnap=snap, textureFilter=filt, winW=ww, winH=wh}")
    check("window mode restored",
          isinstance(final, dict) and final.get("mode") == orig_mode,
          f"{final.get('mode') if isinstance(final, dict) else final} "
          f"(was {orig_mode})")
    # The resolution assertion the earlier version of this script was
    # missing: `setResolution` writes the CONFIG too, so a check that
    # only looked at vsync/msaa/brightness/snap/filter would report a
    # clean restore while the user's saved resolution had been replaced
    # by whatever transient size their window happened to have.
    check("config resolution restored",
          isinstance(final, dict)
          and (int(final.get("width") or -1), int(final.get("height") or -1))
              == (cfg_w, cfg_h),
          f"{final.get('width')}x{final.get('height')} (was {cfg_w}x{cfg_h})")
    check("physical window size restored",
          isinstance(final, dict)
          and (int(final.get("winW") or -1), int(final.get("winH") or -1))
              == (orig_w, orig_h),
          f"{final.get('winW')}x{final.get('winH')} (was {orig_w}x{orig_h})")
    check("every other touched video setting restored",
          isinstance(final, dict)
          and bool(final.get("vsync")) == orig_vsync
          and int(final.get("frameLimit") or 0) == orig_flimit
          and int(final.get("msaa") or 1) == orig_msaa
          and int(final.get("brightness") or 0) == orig_bright
          and bool(final.get("pixelSnap")) == orig_snap
          and final.get("textureFilter") == orig_filter,
          f"{final} (was vsync={orig_vsync} frameLimit={orig_flimit} "
          f"msaa={orig_msaa} brightness={orig_bright} snap={orig_snap} "
          f"filter={orig_filter})")

    print("\nswapchain was rebuilt several times above — eyeball the window "
          "now: it should be rendering normally, at its original size and "
          "window mode, with no artifacts.")
    if failures:
        print(f"video_window_check: FAILED ({len(failures)}): "
              f"{', '.join(failures)}")
        return 1
    print("video_window_check: all checks passed")
    return 0


if __name__ == "__main__":
    sys.exit(main())
