#!/usr/bin/env python3
"""GPU/offscreen lifecycle probe for issue #788: blood GPU textures are
disposed when a world page is replaced or destroyed.

At HEAD before #788, #606's generated blood textures were registered in
the bindless system and their (TextureHandle, cleanup) records stored in
the owning page's ``wsBloodTextureHandlesRef``, but the ONLY sweep that
disposed them (``uploadBloodTextures``) iterates ``wmWorlds`` — so any
page a lifecycle path removed from ``wmWorlds`` leaked its bindless
registrations, Vulkan images/views, and ``textureSizeRef`` entries with
no way to reclaim them. #788 routes each orphaned page's live handle map
to the render thread (``bloodDisposeQueue`` -> ``disposeQueuedBloodTextures``)
for disposal.

Runs under ``--offscreen`` (GPU on, window off): ``uploadBloodTextures``
early-returns with no Vulkan device, so a GPU-less ``--headless`` boot
would exercise nothing. Needs a real Vulkan device, so it is
manual-only / needs-gpu in ``tools/ci_probes.py``.

Metric: ``blood.gpuStats()`` -> {bindless, texSize, bloodHandles}. The
first two are engine-wide, but the only per-page GPU textures churned by
this probe are blood (world tile textures load once at startup; the zoom
atlas and world preview are only produced by the atlas LOD / create-world
UI, neither of which this probe drives), so the deltas are blood-only.

For each teardown/replacement path — single-page destroy, destroy-all,
same-id ``world.init`` (real world), arena replacement, and save-load
replacement — the probe:
  1. puts K distinct blood textures on the active page and confirms
     bindless + texSize each rose by exactly K and bloodHandles == K;
  2. runs the teardown;
  3. confirms bindless + texSize returned to the pre-spawn baseline
     (a fixed leak would leave them at baseline+K) — proving the stale
     bindless registrations and textureSizeRef entries are gone.
Finally it recreates the page, spawns blood again, and confirms it
uploads (bloodHandles rises) and renders (blood.getRenderQuads non-empty).

PASS = every path returns to baseline and blood re-renders after recreate.
FAIL = any path leaves resources above baseline (the leak), or re-render
       fails.
"""
from __future__ import annotations
import argparse
import sys
import time

from probelib import boot, quit_engine, send, send_json

PORT = 9026
LOG = "/tmp/blood_gpu_lifecycle_probe_engine.log"
PAGE = "main_world"
K = 4  # distinct blood textures per cycle (well under defaultBloodTextureCap=24)

# Distinct style buckets -> distinct texture descriptors (isNew each).
STYLES = ["pool", "drops", "spatter", "streak", "smear"]

failures: list[str] = []


def check(cond: bool, msg: str) -> None:
    print(("  ok: " if cond else "  FAIL: ") + msg)
    if not cond:
        failures.append(msg)


def gpu_stats() -> dict:
    s = send_json(PORT, "return blood.gpuStats()")
    if not isinstance(s, dict):
        print(f"FAIL (setup): blood.gpuStats() -> {s!r}")
        quit_engine(PORT)
        sys.exit(2)
    return s


def spawn_distinct(k: int) -> int:
    """Spawn k distinct-style blood decals on the active page; return the
    number reported as new textures."""
    new = 0
    for i in range(k):
        style = STYLES[i % len(STYLES)]
        lua = (f"local d,t,n = blood.spawn({10 + i}.5, {10 + i}.5, 'stab', "
               f"'severe', {{style='{style}', seed={100 + i}}}); return n")
        if send_json(PORT, lua) is True:
            new += 1
    return new


def wait_until(pred, timeout: float, interval: float = 0.25) -> dict:
    """Poll gpu_stats() until pred(stats) or timeout; return last stats."""
    deadline = time.time() + timeout
    stats = gpu_stats()
    while not pred(stats) and time.time() < deadline:
        time.sleep(interval)
        stats = gpu_stats()
    return stats


def wait_stable(timeout: float = 15, interval: float = 0.6) -> dict:
    """Poll until the engine-wide bindless/texSize counts stop changing
    (two consecutive equal reads). One-time world-texture uploads (first
    real world's tile/flora atlases, etc.) settle asynchronously; a
    baseline must be captured AFTER that churn so the blood delta is
    exactly K. Returns the stable stats."""
    deadline = time.time() + timeout
    prev = gpu_stats()
    while time.time() < deadline:
        time.sleep(interval)
        cur = gpu_stats()
        if cur["bindless"] == prev["bindless"] and cur["texSize"] == prev["texSize"]:
            return cur
        prev = cur
    return prev


def fresh_page() -> None:
    """(Re)create a clean arena page under PAGE and show it — 0 blood —
    and wait for engine-wide texture churn to settle before the caller
    baselines."""
    send(PORT, f"world.initArena('{PAGE}')", expect_result=False)
    send(PORT, "return world.waitForInit(60)", timeout=70)
    send(PORT, f"world.show('{PAGE}')", expect_result=False)
    wait_until(lambda s: s["bloodHandles"] == 0, timeout=5)
    wait_stable()


def run_path(name: str, teardown_lua: str, wait_after: float,
             leaves_page: bool) -> None:
    """One teardown/replacement path: spawn K blood, tear the page down,
    confirm resources return to the pre-spawn baseline.

    leaves_page: True if the teardown leaves a live active page under PAGE
    (the replacement paths) so bloodHandles is observable as 0 afterward;
    False if it removes the page entirely (destroy / destroy-all)."""
    print(f"\n== path: {name} ==")
    fresh_page()
    base = gpu_stats()
    check(base["bloodHandles"] == 0, "baseline: active page has 0 blood handles")

    spawn_distinct(K)
    up = wait_until(lambda s: s["bloodHandles"] >= K, timeout=15)
    check(up["bloodHandles"] == K, f"{K} blood textures uploaded (bloodHandles=={K})")
    check(up["bindless"] == base["bindless"] + K,
          f"bindless rose by exactly {K} ({base['bindless']}->{up['bindless']})")
    check(up["texSize"] == base["texSize"] + K,
          f"texSize rose by exactly {K} ({base['texSize']}->{up['texSize']})")

    # Tear the page down and wait for the world command + the render
    # thread's dispose-queue drain to complete.
    send(PORT, teardown_lua, expect_result=False)
    post = wait_until(
        lambda s: s["bindless"] == base["bindless"] and s["texSize"] == base["texSize"],
        timeout=wait_after)

    check(post["bindless"] == base["bindless"],
          f"bindless returned to baseline after teardown "
          f"(leak would be {base['bindless'] + K}, got {post['bindless']})")
    check(post["texSize"] == base["texSize"],
          f"texSize returned to baseline after teardown "
          f"(leak would be {base['texSize'] + K}, got {post['texSize']})")
    if leaves_page:
        check(post["bloodHandles"] == 0,
              "replacement page's blood handle map is empty")


def main() -> int:
    global PORT
    ap = argparse.ArgumentParser()
    ap.add_argument("--port", type=int, default=PORT)
    ap.add_argument("--size", default="1280x720")
    ap.add_argument("--seed", type=int, default=42)
    args = ap.parse_args()
    PORT = args.port

    proc = boot(PORT, LOG, args=["--size", args.size],
                mode=("--offscreen",), label="offscreen-engine")
    try:
        # A real saved world for the load-replacement path (an arena page
        # can't be a save page — #365 hangs on load). No blood in it; the
        # load just has to REPLACE a blood-bearing main_world.
        print("== preparing a save for the load-replacement path ==")
        send(PORT, f"world.init('save_src', {args.seed}, 32, 3)", expect_result=False)
        send(PORT, "return world.waitForInit(180)", timeout=190)
        send(PORT, "return engine.saveWorld('save_src', 'blood_lifecycle_save')",
             timeout=30)
        send(PORT, "world.destroy('save_src')", expect_result=False)
        time.sleep(1.0)

        run_path("single-page destroy",
                 f"world.destroy('{PAGE}')", wait_after=15, leaves_page=False)
        run_path("destroy-all (Exit to Menu)",
                 "world.destroyAll()", wait_after=15, leaves_page=False)
        run_path("same-id normal world init (replace)",
                 f"world.init('{PAGE}', {args.seed}, 32, 3)",
                 wait_after=60, leaves_page=True)
        run_path("arena replacement (replace)",
                 f"world.initArena('{PAGE}')", wait_after=30, leaves_page=True)
        run_path("save-load replacement",
                 "engine.loadSave('blood_lifecycle_save')",
                 wait_after=60, leaves_page=True)

        # Recreate + re-render: blood must still upload and render after a
        # page id has been through teardown.
        print("\n== recreate page + confirm blood re-renders ==")
        fresh_page()
        base = gpu_stats()
        spawn_distinct(K)
        up = wait_until(lambda s: s["bloodHandles"] >= K, timeout=15)
        check(up["bloodHandles"] == K, "blood re-uploads after page recreate")
        check(up["bindless"] == base["bindless"] + K,
              "bindless rises again on the recreated page (fresh upload works)")
        quads = send_json(PORT, "return blood.getRenderQuads()") or []
        check(len(quads) >= K, f"blood.getRenderQuads() renders the marks (got {len(quads)})")
    finally:
        # The 'blood_lifecycle_save' slot is left under the gitignored
        # saves/ dir; harmless and overwritten on the next run.
        quit_engine(PORT, proc)

    print()
    if failures:
        print(f"FAIL: {len(failures)} check(s) failed")
        for f in failures:
            print(f"  - {f}")
        return 1
    print("PASS: blood GPU textures reclaimed on every teardown/replacement path")
    return 0


if __name__ == "__main__":
    sys.exit(main())
