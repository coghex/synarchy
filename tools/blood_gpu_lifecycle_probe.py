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

Metrics. ``blood.gpuStats()`` -> {bindless, texSize, bloodHandles}:
the first two are engine-wide totals, the third the ACTIVE page's blood
handle-map size. ``blood.gpuHandles([handles])`` (#1585) is the
ownership view the totals cannot give: the blood-OWNED texture handles
themselves, each with its membership in the bindless handle map and in
the texture-size cache separately. Texture handles are monotonically
allocated and never reused, so a captured handle still resident after a
teardown is a leak of THAT resource and of nothing else.

Every path spawns K distinct blood textures on the active page and
confirms bindless + texSize each rose by exactly K and bloodHandles == K.
The teardown oracle then splits, because the two situations are not
comparable:

  Four COMPARABLE paths — single-page destroy, destroy-all, same-id
  ``world.init`` (real world), arena replacement. Nothing else in the
  engine allocates a GPU texture across these (world tile textures load
  once at startup; the zoom atlas and world preview are produced by the
  atlas LOD / create-world UI, neither of which this probe drives), so
  the engine-wide totals ARE a valid blood delta: each must return to
  the exact pre-spawn baseline, and a fixed leak would leave it at
  baseline+K.

  SAVE-LOAD replacement. A published load brings up a complete
  replacement session, which registers its own preview texture, zoom
  atlas and world texture set — non-blood registrations that move the
  same engine-wide totals. Comparing them to a pre-load baseline
  compares two different sessions, so it can fail with every blood
  resource correctly reclaimed (issue #1585 / CTV-5). This path
  therefore captures the old page's blood-owned handles BEFORE the load
  and, after confirming THAT load request published, requires every one
  of them to be gone from BOTH registries. Disposal drops the bindless
  registration and the size entry separately, so a handle left in either
  one is a (partial) leak. The replacement page's empty blood map is
  still checked, but it is corroboration, not the verdict: a fresh
  page's map is empty whether or not the old page leaked.

Finally the probe recreates the page, spawns blood again, and confirms
it uploads (bloodHandles rises) and renders (blood.getRenderQuads
non-empty).

PASS = every path reclaims its blood resources and blood re-renders
       after recreate.
FAIL = a comparable path leaves the totals above baseline, the save-load
       path leaves a captured handle resident in either registry, the
       load never publishes, or re-render fails.
"""
from __future__ import annotations
import argparse
import sys
import time

from probelib import (boot, capture_request_id, quit_engine, send,
                      send_json, wait_load_published)

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


def blood_handles(arg: str = ""):
    """``blood.gpuHandles(arg)`` rows (#1585), or None if the verb
    answered nil / the console did not return a table at all.

    An EMPTY Lua table serializes as the JSON object ``{}`` — the debug
    console's array detection needs at least one ``1..n`` key — so an
    empty dict here means "no rows", not a malformed answer."""
    v = send_json(PORT, f"return blood.gpuHandles({arg})")
    if isinstance(v, list):
        return v
    if isinstance(v, dict) and not v:
        return []
    return None


def handle_array(handles: list[int]) -> str:
    """A Lua array literal for the explicit blood.gpuHandles form."""
    return "{" + ",".join(str(h) for h in handles) + "}"


def spawn_phase(name: str) -> tuple[dict, dict]:
    """The half every path shares: a clean baselined page, K distinct
    blood textures on it, and the exact upload-delta assertions.

    Returns (pre-spawn baseline stats, post-upload stats)."""
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
    return base, up


def run_path(name: str, teardown_lua: str, wait_after: float,
             leaves_page: bool) -> None:
    """One COMPARABLE teardown/replacement path: spawn K blood, tear the
    page down, confirm the engine-wide counters return to the pre-spawn
    baseline.

    Sound only because nothing else allocates a GPU texture across these
    paths, which makes the totals a valid blood delta. The save-load
    path breaks that premise and has its own oracle
    ('run_save_load_path'); do not route it through here.

    leaves_page: True if the teardown leaves a live active page under PAGE
    (the replacement paths) so bloodHandles is observable as 0 afterward;
    False if it removes the page entirely (destroy / destroy-all)."""
    base, _ = spawn_phase(name)

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


def run_save_load_path(slot: str, wait_after: float) -> None:
    """The save-load replacement path, judged by OWNERSHIP (#1585).

    A published load brings up a complete replacement session whose own
    preview, zoom-atlas and world-texture registrations move the same
    engine-wide counters blood does, so 'run_path''s return-to-baseline
    comparison would be comparing two different sessions. This oracle
    instead captures the outgoing page's blood-owned texture handles
    first and requires each of them, individually, to be gone from the
    bindless map AND from the texture-size cache — the two registrations
    'World.Render.BloodQuads.disposeBloodRecord' drops separately, so a
    handle left in either one is a leak. Handles are never reused, so no
    allocation the replacement session makes can mask or fake this."""
    name = "save-load replacement"
    base, _ = spawn_phase(name)

    owned = blood_handles()
    if owned is None or len(owned) != K:
        check(False, f"captured the outgoing page's {K} blood-owned GPU "
                     f"handles before the load (blood.gpuHandles() -> {owned!r})")
        return
    check(all(r.get("bindless") and r.get("texSize") for r in owned),
          f"all {K} captured blood handles are resident in BOTH registries "
          f"before the load ({owned})")
    handles = [r["handle"] for r in owned]
    arg = handle_array(handles)

    # Wait for THIS load request to publish (#763): loadSave only ACCEPTS
    # synchronously, and passing the captured request id is what stops a
    # terminal status left by an earlier transaction from satisfying the
    # wait. Nothing about the replacement session may be judged before it.
    accepted = send(PORT, f"return engine.loadSave('{slot}')").strip()
    if accepted != "true":
        check(False, f"engine.loadSave('{slot}') was accepted (returned "
                     f"{accepted!r}; the reason is logged in {LOG})")
        return
    rid = capture_request_id(PORT, "return engine.getLoadStatus()")
    if rid is None:
        check(False, f"engine.getLoadStatus() reported a request id for "
                     f"loadSave('{slot}')")
        return
    published, status = wait_load_published(PORT, wait_after, request_id=rid)
    check(published, f"the load published (request {rid})"
                     + ("" if published else f"; last status {status}"))
    if not published:
        return

    # Publication only means the replacement session is live; the old
    # page's blood records are disposed by the render thread's queue
    # drain, which runs after.
    deadline = time.time() + wait_after
    rows = blood_handles(arg)
    while time.time() < deadline:
        if rows is not None and not any(r.get("bindless") or r.get("texSize")
                                        for r in rows):
            break
        time.sleep(0.25)
        rows = blood_handles(arg)

    if rows is None or len(rows) != len(handles):
        check(False, f"blood.gpuHandles({arg}) answered about all "
                     f"{len(handles)} captured handles (got {rows!r})")
        return
    still_bindless = [r["handle"] for r in rows if r.get("bindless")]
    still_texsize = [r["handle"] for r in rows if r.get("texSize")]
    ok_bindless = not still_bindless
    ok_texsize = not still_texsize
    check(ok_bindless,
          "every pre-load blood handle is gone from the bindless map"
          + ("" if ok_bindless else f" (still registered: {still_bindless})"))
    check(ok_texsize,
          "every pre-load blood handle is gone from the texture-size cache"
          + ("" if ok_texsize else f" (still cached: {still_texsize})"))

    after = gpu_stats()
    if not (ok_bindless and ok_texsize):
        # Requirement 4: say plainly that the totals are NOT the verdict,
        # so a reader cannot mistake replacement-session growth for the
        # leak. World textures land in both registries; the preview and
        # zoom atlas move the bindless map without touching the size
        # cache -- so neither total is attributable to any one source.
        print(f"  context: captured blood handles {handles}; engine-wide "
              f"totals bindless {base['bindless']}->{after['bindless']}, "
              f"texSize {base['texSize']}->{after['texSize']}. Those totals "
              f"are NOT the verdict: the replacement session registers its "
              f"own non-blood textures (world textures in both registries, "
              f"the preview and zoom atlas in the bindless map only), so "
              f"they move whether or not blood leaked. The leak is the "
              f"per-handle residency above.")

    # Corroboration only (#1585 requirement 2): a fresh page's blood map
    # is empty whether or not the OLD page's handles were released, so
    # this can never be the assertion that carries the path.
    check(after["bloodHandles"] == 0,
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
        run_save_load_path("blood_lifecycle_save", wait_after=60)

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
