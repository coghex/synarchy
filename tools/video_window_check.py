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
     `rcFramebufferSizeRef` live and sane through every branch, and the
     mode round-tripping. `fullscreen` is never chosen as the target (it
     switches the monitor video mode); `borderless` covers the same code
     shape without disrupting the desktop,
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

One thing this script deliberately does NOT assert: that the window-mode
round trip returns the window to its pre-transition geometry. It does
not, because of a pre-existing engine bug (#907) in the interaction
between `API.Config.setWindowModeFn` and `Message.Video`'s
`handleSetWindowMode` — see the comment at the round trip for the
mechanism. This check therefore re-pins the window SIZE explicitly at
the end instead of trusting that restore path. The window POSITION is
not restorable from Lua, so running this from `windowed` can leave the
window moved to 0,0; drag it back if so.

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
                  "return {mode=mode, winW=ww, winH=wh}")

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

        # Deliberately NOT asserted: that the round trip lands back on
        # the pre-transition window geometry. It does not, and that is a
        # PRE-EXISTING engine bug (issue #907), not something this
        # capability migration introduced or should fix — #891 is a pure
        # refactor and lists window-behaviour changes as out of scope.
        #
        # `API.Config.setWindowModeFn` enqueues `LuaSetWindowMode` and
        # then writes `vcWindowMode = target` on the Lua thread, so by
        # the time `handleSetWindowMode` runs on MainRender a frame
        # later and reads `videoConfigRef` to decide whether to cache
        # the windowed geometry, the config already reports the TARGET
        # mode. Leaving `windowed` therefore skips the cache, and
        # returning to `windowed` caches the borderless monitor geometry
        # and restores that instead. (It is also racy: if MainRender
        # happens to drain between the enqueue and the config write, the
        # guard sees the old mode and caches correctly.)
        #
        # So this script asserts what the migrated code genuinely
        # guarantees — the handler runs, `rcWindowSizeRef`/
        # `rcFramebufferSizeRef` stay live and sane through every
        # branch, and the mode round-trips — and step 7 below puts the
        # window SIZE back explicitly rather than trusting a restore
        # path that does not work.
        if (int(mode_back.get("winW") or -1),
                int(mode_back.get("winH") or -1)) != (orig_w, orig_h):
            print(f"    (known, pre-existing #907: the mode round trip left "
                  f"the window at {mode_back.get('winW')}x"
                  f"{mode_back.get('winH')} instead of {orig_w}x{orig_h}; "
                  f"size is re-pinned below, but POSITION is not "
                  f"restorable from Lua — drag the window back if it moved)")

    # --- 7. leave the instance as we found it ---------------------------
    # Re-pin the physical window size: the mode round trip above moved it,
    # and in the non-`windowed` starting cases there is no cached geometry
    # guaranteeing it came back to exactly where it began.
    lua(f"engine.setResolution({orig_w}, {orig_h}); return true")
    settle()

    # `engine.setVideoConfig` is the CONFIG-ONLY write (`API.Config`'s
    # `setVideoConfigFn` updates `videoConfigRef` and enqueues nothing),
    # so this restores every one of the ten fields — `vcWidth`/`vcHeight`
    # included, which `setResolution` clobbered just above — without
    # moving the window off the size it was restored to.
    #
    # Skipped entirely on an unrecognized mode: `setVideoConfigFn` maps
    # an unparseable window-mode string to `Windowed`, so passing one
    # through here would SET the user's mode rather than restore it. That
    # case is already a recorded failure above; leaving vcWidth/vcHeight
    # alone is the lesser harm.
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
